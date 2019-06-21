
#region " using "
using System;
using System.Data;
using System.Web;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
using System.Reflection;
#endregion

public partial class trans_receipt_list : System.Web.UI.Page
{
    

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "item_daily_receipt.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            //lblComp.Text = PrmComp;
            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            lblComp.Text = PrmComp;
        }
        
      
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;

            }


          
        }
        try
        {
            Session["Rowuser"] = UserLogin.UserName;
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            //ddl_display.Text = Convert.ToString(Session["gridsize"]);
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);

            GridView1.SelectedIndex = Convert.ToInt32(Session["trans_recept_list_index"]);
            if (Session["trans_recept_list_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["trans_recept_list_seq"] = ((Label)GridView1.SelectedRow.FindControl("reckeylabel")).Text;
            }
            txt_seqno.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_tag.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_date.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_po.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_item.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
            txt_job.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch { }
        try
        {
            TextBox vsearch = (TextBox)FormView2.FindControl("aLineLabel");
            vsearch.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch
        { }

        if (Request.QueryString["addnew"] == "queryadd")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            PropertyInfo isreadonly = typeof(System.Collections.Specialized.NameValueCollection).GetProperty("IsReadOnly", BindingFlags.Instance | BindingFlags.NonPublic);
            // make collection editable
            isreadonly.SetValue(this.Request.QueryString, false, null);
            // remove
            this.Request.QueryString.Remove("addnew");
            // modify
            //this.Request.QueryString.Set("bar", "123");
        }

    }

    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }
    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
    }
    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }



    protected void btnShowAll_Click(object sender, System.EventArgs e)
    {
        
        string ss = "";
        txt_seqno.Text = ss.ToString();       
        txt_tag.Text = ss.ToString();
        txt_po.Text = ss.ToString();
        txt_item.Text = ss.ToString();
        txt_date.Text = ss.ToString();

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource3.SelectParameters["prmSeqno"].DefaultValue = txt_seqno.Text.Trim();
        ObjectDataSource3.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource3.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource3.SelectParameters["prmFgItem"].DefaultValue = txt_item.Text.Trim();
        ObjectDataSource3.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSource3.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();


        Session["trans_recept_list_seq_no"] = txt_seqno.Text.Trim();
        Session["trans_recept_list_recdate"] = txt_date.Text.Trim();
        Session["trans_recept_list_po"] = txt_po.Text.Trim();
        Session["trans_recept_list_inum"] = txt_item.Text.Trim();
        Session["trans_recept_list_tag"] = txt_tag.Text.Trim();
        Session["trans_recept_list_job"] = txt_job.Text.Trim();
    }
   


    protected void btnSearch_Click(object sender, System.EventArgs e)
    {
       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource3.SelectParameters["prmSeqno"].DefaultValue = txt_seqno.Text.Trim();
        ObjectDataSource3.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSource3.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSource3.SelectParameters["prmFgItem"].DefaultValue = txt_item.Text.Trim();
        ObjectDataSource3.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSource3.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

        Session["trans_recept_list_seq_no"] = txt_seqno.Text.Trim();
        Session["trans_recept_list_recdate"] = txt_date.Text.Trim();
        Session["trans_recept_list_po"] = txt_po.Text.Trim();
        Session["trans_recept_list_inum"] = txt_item.Text.Trim();
        Session["trans_recept_list_tag"] = txt_tag.Text.Trim();
        Session["trans_recept_list_job"] = txt_job.Text.Trim();

    }



    protected void hlkBackToMenu_Click(object sender, EventArgs e)
    {
        string sMenuURL = ConfigurationManager.AppSettings["MenuFile"];
        if (sMenuURL == String.Empty)
        {
            Response.Write("<script language=javascript>alert('Menu page isn't set');</script>");
            return;
        }

        
        Response.Redirect(sMenuURL);
    }



    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);
       

    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["trans_recept_list_index"] = GridView1.SelectedIndex;
        Session["trans_recept_list_seq"] = ((Label)GridView1.SelectedRow.FindControl("reckeylabel")).Text;
    }

    protected void lnk_list_click(object sender, EventArgs e)
    {
        Response.Redirect("trans_receipt_list.aspx");
    }

    protected void lnk_view_click(object sender, EventArgs e)
    {
        Response.Redirect("view_trans_receipt.aspx");
    }

    protected void Insert_Button_Click(object sender, EventArgs e)
    {
        TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJobnoTextBox");
        TextBox job2 = (TextBox)FormView1.FindControl("vJobno2TextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("vcustTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox toloc = (TextBox)FormView1.FindControl("vLoc2TextBox");
        TextBox tolocbbin = (TextBox)FormView1.FindControl("vLocBin2TextBox");
        TextBox totag = (TextBox)FormView1.FindControl("vTag2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        Label vdate = (Label)FormView1.FindControl("vDateTextBox");
        Label vtime = (Label)FormView1.FindControl("vTransTimeTextBox");
        Label reckey = (Label)FormView1.FindControl("vRecKeyLabel");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        itemhistory itemh = new itemhistory();
        DataSet ds = new DataSet();
        bool vbool = itemh.validtrnsViewRece(UserLogin.UserName, "validadd", item.Text, job.Text, vdate.Text, tag.Text, vtime.Text.Trim(), job2.Text, "", loc.Text, locbin.Text, cases.Text, qtycas.Text, cust.Text, partial.Text, toloc.Text, tolocbbin.Text , totag.Text, "", "", 0, "");
        string check = Convert.ToString(vbool);
        if (check == "True")
        {
            try
            {

                //Session["trans_recept_list_seq"] = reckey.Text.Trim();

                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Addnewrcpt";
                ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = vdate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = vtime.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = job2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc2"].DefaultValue = toloc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin2"].DefaultValue = tolocbbin.Text.Trim();

                ObjectDataSource1.SelectParameters["prmTagno2"].DefaultValue = totag.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = reckey.Text.Trim();
                FormView1.ChangeMode(FormViewMode.ReadOnly);

                Response.Write("<script>window.location.href='trans_receipt_list.aspx'</script>");
            }
            catch { }
        }

    }
    protected void Update_Button_Click(object sender, EventArgs e)
    {
        TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJobnoTextBox");
        TextBox job2 = (TextBox)FormView1.FindControl("vJobno2TextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("vcustTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox toloc = (TextBox)FormView1.FindControl("vLoc2TextBox");
        TextBox tolocbbin = (TextBox)FormView1.FindControl("vLocBin2TextBox");
        TextBox totag = (TextBox)FormView1.FindControl("vTag2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        Label vdate = (Label)FormView1.FindControl("vDateTextBox");
        Label vtime = (Label)FormView1.FindControl("vTransTimeTextBox");
        Label reckey = (Label)FormView1.FindControl("vRecKeyLabel");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        itemhistory itemh = new itemhistory();
        DataSet ds = new DataSet();
        bool vbool = itemh.validtrnsViewRece(UserLogin.UserName, "validupdate", item.Text, job.Text, vdate.Text, tag.Text, vtime.Text.Trim(), job2.Text, "", loc.Text, locbin.Text, cases.Text, qtycas.Text, cust.Text, partial.Text, toloc.Text, tolocbbin.Text , totag.Text, "", "", 0, "");
        string check = Convert.ToString(vbool);
        if (check == "True")
        {
            try
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = vdate.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = vtime.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = job2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = cust.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc2"].DefaultValue = toloc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin2"].DefaultValue = tolocbbin.Text.Trim();

                ObjectDataSource1.SelectParameters["prmTagno2"].DefaultValue = totag.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = reckey.Text.Trim();
                FormView1.ChangeMode(FormViewMode.ReadOnly);

                Response.Write("<script>window.location.href='trans_receipt_list.aspx'</script>");
            }
            catch { }
        }

    }
    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["trans_recept_list_seq"]);

        Response.Write("<script>window.location.href='trans_receipt_list.aspx'</script>");

    }

    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label sql = (Label)FormView1.FindControl("vRecLabel");
            Session["trans_recept_list_seq"] = sql.Text;

        }
        catch { }
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox unit = (TextBox)FormView1.FindControl("vCasesTextBox");
            unit.Focus();

        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
            tag.Focus();


            Label vdate = (Label)FormView1.FindControl("vDateTextBox");
            Label vtime = (Label)FormView1.FindControl("vTransTimeTextBox");
            Label vucrt = (Label)FormView1.FindControl("vusrcrtTextBox");
            Label vupdt = (Label)FormView1.FindControl("vusrupdtTextBox");
            TextBox blank = (TextBox)FormView1.FindControl("vblankTextBox");
            vdate.Text = DateTime.Now.ToShortDateString();
            vtime.Text = DateTime.Now.ToString("HH:MM");
            vucrt.Text = UserLogin.UserName;
            vupdt.Text = UserLogin.UserName;


        }
    }


    protected void TagTextBox_Change(object sender, EventArgs e)
    {
        TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJobnoTextBox");
        TextBox job2 = (TextBox)FormView1.FindControl("vJobno2TextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cust = (TextBox)FormView1.FindControl("vcustTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox toloc = (TextBox)FormView1.FindControl("vLoc2TextBox");
        TextBox tolocbbin = (TextBox)FormView1.FindControl("vLocBin2TextBox");
        TextBox totag = (TextBox)FormView1.FindControl("vTag2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");

        try
        {
            browspo look = new browspo();
            DataSet ds = new DataSet();
            ds = look.SelecttrnstagLook("PoSearch", Convert.ToString(Session["User"]), "Tag#", "EQUAL", tag.Text.Trim(), "", "");
            item.Text = ds.Tables[0].Rows[0][3].ToString();
            itemname.Text = ds.Tables[0].Rows[0][4].ToString();
            job.Text = ds.Tables[0].Rows[0][5].ToString();
            job2.Text = ds.Tables[0].Rows[0][6].ToString();
            loc.Text = ds.Tables[0].Rows[0][7].ToString();
            locbin.Text = ds.Tables[0].Rows[0][8].ToString();
            cases.Text = ds.Tables[0].Rows[0][11].ToString();
            qtycas.Text = ds.Tables[0].Rows[0][9].ToString();
            totag.Text = ds.Tables[0].Rows[0][17].ToString();
            partial.Text = ds.Tables[0].Rows[0][14].ToString();
            cases.Focus();
        }
        catch
        {
            HttpContext.Current.Response.Write("<script>alert('Invalid Tag# Number')</script>");

        }

    }
    protected void Cancel_button_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }

}
