
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Collections;
using System.Configuration;
using System.Web;
using System.Threading;
using System.Globalization;
using System.Text;
#endregion

public partial class distribute_account : System.Web.UI.Page
{
    string oldinv = "";
    string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "distribute_account.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {

                if (Session["gl_dist_reckey_rec_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["gl_dist_reckey_rec_index"]);
                        Session["gl_account_dist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["gl_account_dist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }
            

        } //  ! Page.IsPostBack

        //StringBuilder str = new StringBuilder();
        //str.Append("<script language=javascript>");
        //str.Append("function update(e){");

        //str.Append("document.forms[0].FormView2_actnumTextBox.value=e[1];");
        //str.Append("var line = document.getElementById('FormView2_arlineLabel');");
        //str.Append("line.value = e[0];");
        //str.Append("document.forms[0].FormView2_inv_qtyTextBox.value=e[2];");
        //str.Append("var tmsf2 = document.getElementById('FormView2_totl_msfLabel');");
        //str.Append("tmsf2.value = e[3];");
        //str.Append("var ino = document.getElementById('FormView2_i_noLabel');");
        //str.Append("ino.value = e[4];");
        //str.Append("var sn = document.getElementById('FormView2_snumLabel');");
        //str.Append("sn.value = e[5];");
        //str.Append("var accdesc = document.getElementById('FormView2_actdscrlabel');");
        //str.Append("accdesc.value = e[6];}");

        //str.Append("</script>");

        //// register the javascript into the Page
        //if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "update"))
        //{
        //    Page.RegisterClientScriptBlock("update", str.ToString());
        //}
       
       


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

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_gl_account.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("gl_account_list.aspx");
    }

    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["gl_dist_reckey_rec_index"] = GridView1.SelectedIndex;
        Session["gl_account_dist_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox act = (TextBox)FormView2.FindControl("csact_TextBox2");
        Label dscr = (Label)FormView2.FindControl("csactdscr_label");
        TextBox pct = (TextBox)FormView2.FindControl("pct_TextBox");
        Label type = (Label)FormView2.FindControl("type_label");       

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];

       
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource3.SelectParameters["prmcst_act"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcst_dscr"].DefaultValue = dscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmc_rate"].DefaultValue = pct.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactype"].DefaultValue = type.Text.Trim();                
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='distribute_account.aspx'</script>"); 
          
    }

    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox act = (TextBox)FormView2.FindControl("csact_TextBox");
        Label dscr = (Label)FormView2.FindControl("csactdscr_label");
        TextBox pct = (TextBox)FormView2.FindControl("pct_TextBox");
        Label type = (Label)FormView2.FindControl("type_label");       

        

        UserClass UserLogin = (UserClass)Session["User"];

                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";                                
                ObjectDataSource3.SelectParameters["prmcst_act"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmcst_dscr"].DefaultValue = dscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmc_rate"].DefaultValue = pct.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactype"].DefaultValue = type.Text.Trim();                
               

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='distribute_account.aspx'</script>");               
            
        

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            

            

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox actnum = (TextBox)FormView2.FindControl("csact_TextBox2");
            actnum.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["gl_account_dist_reckey"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;
            try
            {
                //if (FormView2.DataItemCount.ToString() == "0")
                    //AddNewFormView2Button.Visible = true;
                //else
                    //AddNewFormView2Button.Visible = false;
            }
            catch { }
        }
    }

    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {

        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");


        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Delete";
       
        Response.Write("<script>window.location.href='distribute_account.aspx'</script>");


    }

    //protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    //{
    //    FormView2.ChangeMode(FormViewMode.Insert);
    //    //AddNewFormView2Button.Visible = false;
    //}

    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

    protected void load_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("distribute_account.aspx");
    }

    

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        Session["gl_account_list_add_button"] = "add";
        Response.Redirect("view_gl_account.aspx");

    }

    protected void FormView1_ondatabound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            Label actt = (Label)FormView1.FindControl("actTextBox");
            Session["glact_dist_rec"] = actt.Text;
        }
    }

}
