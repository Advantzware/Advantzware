using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Progress.Open4GL.Proxy;
using ASINET1;
using ASIDataNS;
using System.Reflection;

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class recelist_rcpt : System.Web.UI.Page
{
    public recelist_rcpt()
    {
        //
        // TODO: Add constructor logic here
        //
    }
    
    protected void Page_Load(object sender, EventArgs e)
    {
        Session["master_rece_delete_rece_list"] = "rece";
        Session["my_new_rcpt"] = null;
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Session["Rowuser"] = UserLogin.UserName;
        Session["prmUser"] = UserLogin.UserName;       
      
        if (!Page.IsPostBack)
        {

            if (Session["prmSeq_rcpt_gd"] != null)
                txt_seq.Text = Convert.ToString(Session["prmSeq_rcpt_gd"]);
            else
                txt_seq.Text = "";
            if (Session["prmTag_rcpt_gd"] != null)
                txt_tag.Text = Convert.ToString(Session["prmTag_rcpt_gd"]);
            else
                txt_tag.Text = "";
            if (Session["prmDate_rcpt_gd"] != null)
                txt_date.Text = Convert.ToString(Session["prmDate_rcpt_gd"]);
            else
                txt_date.Text = "";
            if (Session["prmPo_rcpt_gd"] != null)
                txt_po.Text = Convert.ToString(Session["prmPo_rcpt_gd"]);
            else
                txt_po.Text = "";
            if (Session["prmIno_rcpt_gd"] != null)
                txt_ino.Text = Convert.ToString(Session["prmIno_rcpt_gd"]);
            else
                txt_ino.Text = "";
            if (Session["prmJob_rcpt_gd"] != null)
                txt_job.Text = Convert.ToString(Session["prmJob_rcpt_gd"]);
            else
                txt_job.Text = "";
            try
            {
                itemhistory hs = new itemhistory();
                DataSet ds = new DataSet();
                ds = hs.ViewRece(UserLogin.UserName, "getnkvalue", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "");
                getvalueTextBox.Text = ds.Tables[0].Rows[0][1].ToString();
            }
            catch { }
        }

        Session["prmAction"] = Convert.ToString(Session["prmAction"]);
        ObjectDataSourcegrid.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                        
        Label heading = (Label)Master.FindControl("Label_heading");
        heading.Text = "FG Warehouse Transactions :";

        //ImageButton brwsorder = (ImageButton)Master.FindControl("recelist_rcpt");
        //brwsorder.ImageUrl = "Images/list-receipts1.jpg";

        if (Session["gridsize"] != null)
        {
            //GridView1.PageSize = Convert.ToInt32(Session["gridsize"]);
        }
        try
        {
            TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
            Session["size"] = Convert.ToInt32(ddl_display.Text);
            GridView1.PageSize = Convert.ToInt32(Session["size"]);
        }
        catch
        {
            return;
        }


        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_gd"]);

        GridView1.SelectedIndex = Convert.ToInt32(Session["list_rcpt_grid_seqno_index_gd"]) - 1;
        try
        {
            if (Session["list_rcpt_grid_seqno_index_gd"] == null)
            {
                GridView1.SelectedIndex = 0;
            }
           
            if (Convert.ToString(Session["addnewrece"]) == "add")
            {
                int rowIndex = 0;
                foreach (GridViewRow row in GridView1.Rows)
                {
                    if (row.Cells[19].Text.ToString().Equals(Convert.ToString(Session["seqno_list"])))
                    {
                        rowIndex = row.RowIndex;
                        break;
                    }
                }
                GridView1.SelectedIndex = rowIndex;
                Session["addnewrece"] = null;
            }
                Session["seqno_list"] = GridView1.SelectedRow.Cells[19].Text;
        }
        catch
        {
           
        }

        txt_seq.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_tag.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_date.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_po.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_ino.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        txt_job.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        

        try
        {
            TextBox vsearch = (TextBox)FormView1.FindControl("aLineLabel");
            vsearch.Attributes.Add("onkeypress", "return clickButton(event,'" + btnSearch.ClientID + "')");
        }
        catch
        { }

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["PrmAction"].DefaultValue = "Select";


       

        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "recelist_rcpt.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
                       

        }
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
        if (Convert.ToString(Session["add_rcpt_list_buton"]) == "Additem")
        {
            FormView1.ChangeMode(FormViewMode.Insert);
            Session["add_rcpt_list_buton"] = "";
        }

    }

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSourcegrid.SelectParameters["prmAction"].DefaultValue = "GridSearch";
        ObjectDataSourcegrid.SelectParameters["prmSeqno"].DefaultValue = txt_seq.Text;
        ObjectDataSourcegrid.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmFgItem"].DefaultValue = txt_ino.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

        Session["prmAction_rcpt_gd"] = "GridSearch";
        Session["prmSeq_rcpt_gd"] = null;
        Session["prmTag_rcpt_gd"] = null;
        Session["prmDate_rcpt_gd"] = null;
        Session["prmPo_rcpt_gd"] = null;
        Session["prmIno_rcpt_gd"] = null;
        Session["prmJob_rcpt_gd"] = null;

        Session["list_rcpt_grid_seqno_index_gd"] = null;
        Session["list_rcpt_grid_seqno_gd"] = null;
        GridView1.SelectedIndex = 0;

    }

    protected void btn_reset_Click(object sender, EventArgs e)
    { 
        ContentPlaceHolder ct = (ContentPlaceHolder)Master.FindControl("ContentPlaceHolder1");
        foreach (Control c in ct.Controls)
        {
            switch (c.GetType().ToString())
            {
                case "System.Web.UI.WebControls.TextBox":
                    ((TextBox)c).Text = "";
                    break;
            }
        }
        
        Session["gridsize"] = null;
        GridView1.SelectedIndex = 0;
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = txt_customer.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = txt_customer.Text.Trim();
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSourcegrid.SelectParameters["prmAction"].DefaultValue = "GridSelect";
        ObjectDataSourcegrid.SelectParameters["prmSeqno"].DefaultValue = txt_seq.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmTagno"].DefaultValue = txt_tag.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmRcptDate"].DefaultValue = txt_date.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmPono"].DefaultValue = txt_po.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmFgItem"].DefaultValue = txt_ino.Text.Trim();
        ObjectDataSourcegrid.SelectParameters["prmJobno"].DefaultValue = txt_job.Text.Trim();

        Session["prmAction_rcpt_gd"] = "GridSelect";
        Session["prmSeq_rcpt_gd"] = null;
        Session["prmTag_rcpt_gd"] = null;
        Session["prmDate_rcpt_gd"] = null;
        Session["prmPo_rcpt_gd"] = null;
        Session["prmIno_rcpt_gd"] = null;
        Session["prmJob_rcpt_gd"] = null;

        Session["list_rcpt_grid_seqno_index_gd"] = null;
        Session["list_rcpt_grid_seqno_gd"] = null;

        Session["page_inder_gd"] = 0;

        GridView1.PageIndex = Convert.ToInt32(Session["page_inder_gd"]);
    }

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {        
        Session["seqno_list"] = GridView1.SelectedRow.Cells[19].Text;

        Session["list_rcpt_grid_seqno_index_gd"] = GridView1.SelectedIndex + 1;
        foreach (GridViewRow gv in GridView1.Rows)
        {
            //Session["list_rcpt_grid_seqno"] = ((Label)GridView1.SelectedRow.FindControl("Label_seqno")).Text;
            //Session["list_rfq_rfq_nos"] = GridView1.SelectedRow.Cells[1].Text;
            //Session["list_rfq_cust_part_no"] = GridView1.SelectedRow.Cells[5].Text;
            //Session["list_rfq_cust_no"] = GridView1.SelectedRow.Cells[4].Text;
        }        
    }

    protected void ddl_display_TextChanged(object sender, EventArgs e)
    {
        TextBox ddl_display = (TextBox)FormView2.FindControl("aLineLabel");
        Session["gridsize"] = ddl_display.Text;
        //ddl_display.Text = Convert.ToString(Session["gridsize"]);
        ObjectDataSource2.SelectParameters["vLine"].DefaultValue = Convert.ToString(Session["gridsize"]);

    }
    protected void GridView1_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {

        GridView1.PageIndex = e.NewPageIndex;
        Session["page_inder_gd"] = e.NewPageIndex;      
    }


    protected void FormView1_DataBound(object sender, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            try
            {
                Label seq = (Label)FormView1.FindControl("vRnoTextBox");
                TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
                Label create = (Label)FormView1.FindControl("vCreatedByLabel");
                Label updateby = (Label)FormView1.FindControl("vCreate2Label");
                Label transtime = (Label)FormView1.FindControl("vTransTimeLabel");
                date.Text = DateTime.Now.ToShortDateString();
                tag.Focus();
                create.Text = UserLogin.UserName;
                updateby.Text = UserLogin.UserName;
                transtime.Text = DateTime.Now.ToString("HH:MM");

                TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");

                TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
                TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
                TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
                TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
                TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
                TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
                TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
                TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
                TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
                cases.Text = "0";
                qtycas.Text = "0";
                casunit.Text = "0";
                partial.Text = "0";
                stdcost.Text = "0";
                t_qty.Text = "0";
                frtcost.Text = "0";
                extcost.Text = "0";
                tot_wt.Text = "0";

                itemhistory rcpt = new itemhistory();
                DataSet dsrfqs = new DataSet();

                dsrfqs = rcpt.ViewRcpt(UserLogin.UserName, "Addnewrcpt", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "");

                seq.Text = dsrfqs.Tables[0].Rows[0][0].ToString();
            }
            catch { }
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            try
            {
                TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
                tag.Focus();
            }
            catch { }
        }
        // }
        // catch { }
    }

    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Label seq = (Label)FormView1.FindControl("vRnoLabel");
        TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
        // TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
        TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
        TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
        TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
        TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
        TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
        TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
        TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
        TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
        Label create = (Label)FormView1.FindControl("vCreatedByLabel");
        Label updateby = (Label)FormView1.FindControl("vCreate2Label");
        TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");

        itemhistory itemh = new itemhistory();
        DataSet ds = new DataSet();
        bool vbool = itemh.ValidateViewRece(UserLogin.UserName, "ValidateUpdate", item.Text, job.Text, pono.Text, seq.Text, date.Text.Trim(), tag.Text, "", "", "", loc.Text, locbin.Text, cases.Text, "", "", "", "", costuom.Text, "", "", "", stackcode.Text, "", "", "","");
        string check = Convert.ToString(vbool);
        if (check == "True")
        {
            try
            {
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = seq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = transtime.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = jobno2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource1.SelectParameters["prmStdCost"].DefaultValue = stdcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCost_Uom"].DefaultValue = costuom.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTQty"].DefaultValue = t_qty.Text.Trim();
                ObjectDataSource1.SelectParameters["prmFrtCost"].DefaultValue = frtcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmExtCost"].DefaultValue = extcost.Text.Trim();

                ObjectDataSource1.SelectParameters["prmStackCode"].DefaultValue = stackcode.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTotWt"].DefaultValue = tot_wt.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='recelist_rcpt.aspx'</script>");
                
            }
            catch { }
        }



    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        Label seq = (Label)FormView1.FindControl("vRnoTextBox");
        TextBox date = (TextBox)FormView1.FindControl("vDateTextBox");
        //TextBox transtime = (TextBox)FormView1.FindControl("vTransTimeTextBox");
        TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
        TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
        TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
        TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
        TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
        TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
        TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
        TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
        Label create = (Label)FormView1.FindControl("vCreatedByLabel");
        Label updateby = (Label)FormView1.FindControl("vCreate2Label");
        TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
        
        itemhistory itemh = new itemhistory();
        DataSet ds = new DataSet();
        bool vbool = itemh.ValidateViewRece(UserLogin.UserName, "ValidateUpdate", item.Text, job.Text, pono.Text, seq.Text, date.Text.Trim(), tag.Text, "", "", "", loc.Text, locbin.Text, cases.Text, "", "", "", "", costuom.Text, "", "", "", stackcode.Text, "", "", "","");
        string check = Convert.ToString(vbool);
        if (check == "True")
        {
            try
            {
                Session["seqno_list"] = seq.Text.Trim();

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJobno"].DefaultValue = job.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPono"].DefaultValue = pono.Text.Trim();
                ObjectDataSource1.SelectParameters["prmSeqno"].DefaultValue = seq.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRcptDate"].DefaultValue = date.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTagno"].DefaultValue = tag.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTransTime"].DefaultValue = transtime.Text.Trim();
                ObjectDataSource1.SelectParameters["prmJob_no2"].DefaultValue = jobno2.Text.Trim();
                ObjectDataSource1.SelectParameters["prmName"].DefaultValue = itemname.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLoc"].DefaultValue = loc.Text.Trim();
                ObjectDataSource1.SelectParameters["prmLocBin"].DefaultValue = locbin.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCases"].DefaultValue = cases.Text.Trim();
                ObjectDataSource1.SelectParameters["prmQty_Cas"].DefaultValue = qtycas.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCasUnit"].DefaultValue = casunit.Text.Trim();
                ObjectDataSource1.SelectParameters["prmPartial"].DefaultValue = partial.Text.Trim();
                ObjectDataSource1.SelectParameters["prmStdCost"].DefaultValue = stdcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmCost_Uom"].DefaultValue = costuom.Text.Trim();
                ObjectDataSource1.SelectParameters["prmTQty"].DefaultValue = t_qty.Text.Trim();
                ObjectDataSource1.SelectParameters["prmFrtCost"].DefaultValue = frtcost.Text.Trim();
                ObjectDataSource1.SelectParameters["prmExtCost"].DefaultValue = extcost.Text.Trim();

                ObjectDataSource1.SelectParameters["prmStackCode"].DefaultValue = stackcode.Text.Trim();
                //ObjectDataSource1.SelectParameters["prmTotWt"].DefaultValue = tot_wt.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                Session["addnewrece"] = "add";
                Response.Write("<script>window.location.href='recelist_rcpt.aspx'</script>");
                
            }
            catch { }
        }
    }

    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        Label seq = (Label)FormView1.FindControl("vRnoLabel");
        Label item = (Label)FormView1.FindControl("vItemLabel");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Deletercpt";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = seq.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFgItem"].DefaultValue = item.Text.Trim();

            FormView1.ChangeMode(FormViewMode.ReadOnly);
            Session["list_rcpt_grid_seqno_index_gd"] = null;
            Response.Write("<script>window.location.href='recelist_rcpt.aspx'</script>");
        }
        catch { }

    }
    protected void FormView1_Unload(object sender, EventArgs e)
    {

        try
        {
            Label seq = (Label)FormView1.FindControl("vRnoLabel");


            Session["seqno_list"] = seq.Text.Trim();


        }
        catch { }
    }

    protected void InserCancelButton_Click(object sender, EventArgs e)
    {
        Label seq = (Label)FormView1.FindControl("vRnoTextBox");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        try
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Deletercpt";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = seq.Text.Trim();


            FormView1.ChangeMode(FormViewMode.ReadOnly);
        }
        catch { }
    }

    protected void pono_TextChange(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
        TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
        TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
        TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
        TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
        TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
        TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
        Label create = (Label)FormView1.FindControl("vCreatedByLabel");
        Label updateby = (Label)FormView1.FindControl("vCreate2Label");
        TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");

        if (pono.Text.Trim() == "")
        {
            pono.Focus();
            return;
        }
        try
        {
            itemhistory polook = new itemhistory();
            DataSet ds = new DataSet();

            ds = polook.PoSelLookup("PoSearch", UserLogin.UserName, "PO#", "EQUAL", pono.Text.Trim());

            if (ds.Tables[0].Rows.Count == 0)
            {
                HttpContext.Current.Response.Write("<script>alert('Invalid PO#!')</script>");
                pono.Focus();
            }
            else
            {
                item.Text = ds.Tables[0].Rows[0][2].ToString();
                itemname.Text = ds.Tables[0].Rows[0][3].ToString();
                job.Text = ds.Tables[0].Rows[0][4].ToString();
                jobno2.Text = ds.Tables[0].Rows[0][5].ToString();
                costuom.Text = ds.Tables[0].Rows[0][19].ToString();
                stdcost.Text = ds.Tables[0].Rows[0][20].ToString();
                loc.Text = ds.Tables[0].Rows[0][10].ToString();
                locbin.Text = ds.Tables[0].Rows[0][11].ToString();
                qtycas.Text = ds.Tables[0].Rows[0][12].ToString();
                t_qty.Text = ds.Tables[0].Rows[0][22].ToString();
                cases.Text = ds.Tables[0].Rows[0][24].ToString();
                partial.Text = ds.Tables[0].Rows[0][23].ToString();
                casunit.Text = "1";
                if (cases.Text == "0")
                {
                    //partial.Text = "0";
                    //t_qty.Text = "0";
                }
                pono.Focus();
            }
        }
        catch { }
    }

    protected void Tagtextbox_Click(object sneder, EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox tag = (TextBox)FormView1.FindControl("vTagTextBox");
        TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
        TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
        TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
        TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
        TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
        TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
        TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
        Label create = (Label)FormView1.FindControl("vCreatedByLabel");
        Label updateby = (Label)FormView1.FindControl("vCreate2Label");
        TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
        if (tag.Text.Trim() == "")
        {
            tag.Focus();
            return;
        }
        try
        {
            browspo fgtaglook = new browspo();
            DataSet ds = new DataSet();

            ds = fgtaglook.SelectfgtakLook("PoSearch", UserLogin.UserName, "Tag#", "EQUAL", tag.Text.Trim(), "no", "");

            if (ds.Tables[0].Rows.Count == 0)
            {
                HttpContext.Current.Response.Write("<script>alert('Invalid Tag#!' )</script>");
                tag.Focus();
            }
            else
            {
                loc.Text = ds.Tables[0].Rows[0][7].ToString();
                locbin.Text = ds.Tables[0].Rows[0][8].ToString();
                job.Text = ds.Tables[0].Rows[0][5].ToString();
                jobno2.Text = ds.Tables[0].Rows[0][6].ToString();
                item.Text = ds.Tables[0].Rows[0][3].ToString();
                itemname.Text = ds.Tables[0].Rows[0][4].ToString();
                cases.Text = ds.Tables[0].Rows[0][18].ToString();
                qtycas.Text = ds.Tables[0].Rows[0][9].ToString();
                casunit.Text = ds.Tables[0].Rows[0][12].ToString();
                partial.Text = ds.Tables[0].Rows[0][24].ToString();
                stdcost.Text = ds.Tables[0].Rows[0][15].ToString();
                costuom.Text = ds.Tables[0].Rows[0][13].ToString();
                t_qty.Text = ds.Tables[0].Rows[0][16].ToString();
                tag.Focus();
            }
        }
        catch { }
    }

    protected void ItemTextBox_Change(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        TextBox pono = (TextBox)FormView1.FindControl("vPo_noTextBox");
        TextBox job = (TextBox)FormView1.FindControl("vJob_noTextBox");
        TextBox jobno2 = (TextBox)FormView1.FindControl("vJob_no2TextBox");
        TextBox item = (TextBox)FormView1.FindControl("vItemTextBox");
        TextBox itemname = (TextBox)FormView1.FindControl("vItemNameTextBox");
        TextBox loc = (TextBox)FormView1.FindControl("vLocTextBox");
        TextBox locbin = (TextBox)FormView1.FindControl("vLocBinTextBox");
        TextBox cases = (TextBox)FormView1.FindControl("vCasesTextBox");
        TextBox qtycas = (TextBox)FormView1.FindControl("vQtyCasTextBox");
        TextBox casunit = (TextBox)FormView1.FindControl("vCasUnitTextBox");
        TextBox partial = (TextBox)FormView1.FindControl("vPartialTextBox");
        TextBox stdcost = (TextBox)FormView1.FindControl("vStdCostTextBox");
        TextBox costuom = (TextBox)FormView1.FindControl("vCostUomTextBox");
        TextBox t_qty = (TextBox)FormView1.FindControl("vT_QtyTextBox");
        TextBox frtcost = (TextBox)FormView1.FindControl("vFrtCostTextBox");
        TextBox extcost = (TextBox)FormView1.FindControl("vExtCostTextBox");
        TextBox stackcode = (TextBox)FormView1.FindControl("vStackCodeTextBox");
        Label create = (Label)FormView1.FindControl("vCreatedByLabel");
        Label updateby = (Label)FormView1.FindControl("vCreate2Label");
        TextBox tot_wt = (TextBox)FormView1.FindControl("vTot_WtTextBox");
        if (item.Text.Trim() == "")
        {
            item.Focus();
            return;
        }
        try
        {
            itemhistory fglook = new itemhistory();
            DataSet ds = new DataSet();

            ds = fglook.PoSelLookup("ItemSearch", UserLogin.UserName, "Item", "EQUAL", item.Text.Trim());

            if (ds.Tables[0].Rows.Count == 0)
            {

                item.Focus();
            }
            else
            {
                itemname.Text = ds.Tables[0].Rows[0][14].ToString();
                if (qtycas.Text == "" || qtycas.Text == "0")
                    qtycas.Text = ds.Tables[0].Rows[0][12].ToString();
                if (stdcost.Text == "" || stdcost.Text == "0")
                    stdcost.Text = ds.Tables[0].Rows[0][20].ToString();
                if (loc.Text == "")
                    loc.Text = ds.Tables[0].Rows[0][15].ToString();
                if (locbin.Text == "")
                    locbin.Text = ds.Tables[0].Rows[0][16].ToString();
                if (costuom.Text == "")
                    costuom.Text = ds.Tables[0].Rows[0][18].ToString();


                casunit.Text = "1";
                if (cases.Text == "0")
                {
                    //partial.Text = "0";
                    //t_qty.Text = "0";
                }
                item.Focus();
            }
        }
        catch { }

    }

    protected void open_pst(object sender, EventArgs e)
    {
        Response.Redirect("finish_goods_psting.aspx");
    } 


    
}
