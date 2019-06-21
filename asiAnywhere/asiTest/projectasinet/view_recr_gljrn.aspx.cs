
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

public partial class view_recr_gljrn : System.Web.UI.Page
{
    
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "recr_gljrn_list.aspx";
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

                if (Convert.ToString(Session["recr_gljrn_list_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["recr_gljrn_list_add_button"] = null;
                }
                if (Session["view_recrgeneral_ledger_reckey_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_recrgeneral_ledger_reckey_index"]);
                        Session["view_recrgeneral_ledger_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                        Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_recrgeneral_ledger_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                        Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }
            

        } //  ! Page.IsPostBack

             


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox jrnl = (TextBox)FormView1.FindControl("jrnlTextBox");
           TextBox tcrdt = (TextBox)FormView1.FindControl("tcrdtTextBox");
          
           TextBox tdebit = (TextBox)FormView1.FindControl("tdebitTextBox");
           TextBox period = (TextBox)FormView1.FindControl("periodTextBox");
           TextBox bal = (TextBox)FormView1.FindControl("balanTextBox");
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
           DropDownList drop = (DropDownList)FormView1.FindControl("DropDownList1");

           jrnl.Focus();
           drop.SelectedIndex = 2;
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
           try
           {
               UserClass UserLogin = (UserClass)Session["User"];
               ledger con = new ledger();
               DataSet ds = new DataSet();
               //ds = con.RecrGeneralLedgerlist("AddNew", "", UserLogin.UserName, 0, "", 0, 0, 0, 0, "", "", "", "","");

               jrnl.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);               
                              
               reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][10]);
               
              
           }
           catch { }
           
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           Label ny1 = (Label)FormView1.FindControl("reverLabel");
           CheckBox chk1 = (CheckBox)FormView1.FindControl("CheckBox1");
           Label ny2 = (Label)FormView1.FindControl("prereverLabel");
           CheckBox chk2 = (CheckBox)FormView1.FindControl("CheckBox2");
           if (ny1.Text.ToUpper() == "YES")
               chk1.Checked = true;
           if (ny2.Text.ToUpper() == "YES")
               chk2.Checked = true;

           
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {
           
           try
           {
               Label ny1 = (Label)FormView1.FindControl("reverLabel");
               CheckBox chk1 = (CheckBox)FormView1.FindControl("CheckBox1");
               Label ny2 = (Label)FormView1.FindControl("prereverLabel");
               CheckBox chk2 = (CheckBox)FormView1.FindControl("CheckBox2");
               if (ny1.Text.ToUpper() == "YES")
                   chk1.Checked = true;
               if (ny2.Text.ToUpper() == "YES")
                   chk2.Checked = true;
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["recr_general_ledger_reckey_journal"] = reckey.Text.Trim();
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   
   protected void delete_Button_Click(object sender, EventArgs e)
   {

       Label reckey = (Label)FormView1.FindControl("reckeyLabel");
       UserClass UserLogin = (UserClass)Session["User"];

       try
       {
           ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
           ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DataDelete";
           ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

           FormView1.ChangeMode(FormViewMode.ReadOnly);

           Response.Write("<script>window.location.href='view_recr_gljrn.aspx'</script>");
       }
       catch { }

   }

   protected void Formview1_InsertCancelButtonClick(object sender, EventArgs e)
   {
       try
       {
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
           UserClass UserLogin = (UserClass)Session["User"];
           ledger con = new ledger();
           DataSet ds = new DataSet();
           //ds = con.RecrGeneralLedgerlist("DataDelete", "", UserLogin.UserName, 0, "", 0, 0, 0, 0, "", "", "","", reckey.Text.Trim());

       }
       catch { }

   }

   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       TextBox jrnl = (TextBox)FormView1.FindControl("jrnlTextBox");
       TextBox tcrdt = (TextBox)FormView1.FindControl("tcrdtTextBox");
       TextBox tdebit = (TextBox)FormView1.FindControl("tdebitTextBox");
       TextBox period = (TextBox)FormView1.FindControl("periodTextBox");
       TextBox bal = (TextBox)FormView1.FindControl("balanTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
       CheckBox chk1 = (CheckBox)FormView1.FindControl("CheckBox1");
        CheckBox chk2 = (CheckBox)FormView1.FindControl("CheckBox2");
        DropDownList drop = (DropDownList)FormView1.FindControl("DropDownList1");

       if (tcrdt.Text == "")
           tcrdt.Text = "0";
       if (tdebit.Text == "")
           tdebit.Text = "0";
       //if (period.Text == "")
       //    period.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];
       ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
       ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
       ObjectDataSource1.SelectParameters["prmjrn_no"].DefaultValue = jrnl.Text.Trim();
       ObjectDataSource1.SelectParameters["prmreverse"].DefaultValue = Convert.ToString(chk1.Checked);
       ObjectDataSource1.SelectParameters["prmfrom_revr"].DefaultValue = Convert.ToString(chk2.Checked);
       ObjectDataSource1.SelectParameters["prmcb_freq"].DefaultValue = drop.SelectedValue;
       
       

       FormView1.ChangeMode(FormViewMode.ReadOnly);
       

   }

   protected void InsertButton_Click(object sender, EventArgs e)
   {
       TextBox jrnl = (TextBox)FormView1.FindControl("jrnlTextBox");
       TextBox tcrdt = (TextBox)FormView1.FindControl("tcrdtTextBox");
       
       TextBox tdebit = (TextBox)FormView1.FindControl("tdebitTextBox");
       TextBox period = (TextBox)FormView1.FindControl("periodTextBox");
       TextBox bal = (TextBox)FormView1.FindControl("balanTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
       CheckBox chk1 = (CheckBox)FormView1.FindControl("CheckBox1");
       CheckBox chk2 = (CheckBox)FormView1.FindControl("CheckBox2");
       DropDownList drop = (DropDownList)FormView1.FindControl("DropDownList1");
       if (tcrdt.Text == "")
           tcrdt.Text = "0";
       if (tdebit.Text == "")
           tdebit.Text = "0";
       //if (period.Text == "")
       //    period.Text = "0";

       UserClass UserLogin = (UserClass)Session["User"];


                Session["recr_general_ledger_reckey_journal"] = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "AddUpdate";
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmjrn_no"].DefaultValue = jrnl.Text.Trim();
                ObjectDataSource1.SelectParameters["prmreverse"].DefaultValue = Convert.ToString(chk1.Checked);
                ObjectDataSource1.SelectParameters["prmfrom_revr"].DefaultValue = Convert.ToString(chk2.Checked);
                ObjectDataSource1.SelectParameters["prmcb_freq"].DefaultValue = drop.SelectedValue;
                ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = Convert.ToString(Session["recr_general_ledger_reckey_journal"]);

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                FormView2.ChangeMode(FormViewMode.Insert);

          
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
        Response.Redirect("view_recr_gljrn.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("recr_gljrn_list.aspx");
    }

    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_recrgeneral_ledger_reckey_index"] = GridView1.SelectedIndex;
        Session["view_recrgeneral_ledger_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
        Session["top_list_notes_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox line = (TextBox)FormView2.FindControl("lineTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");
        TextBox dscr = (TextBox)FormView2.FindControl("dscrTextBox");
        TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];

        try
        {

            ledger val = new ledger();

            bool check = val.validateGeneralLedgerView("ValidateUpdate", "", UserLogin.UserName, Convert.ToInt32(line.Text.Trim()), act.Text.Trim(), Convert.ToString(Session["general_ledger_reckey_journal"]), dscr.Text.Trim(), Convert.ToDecimal(amt.Text.Trim()), 0, Convert.ToString(Session["view_general_ledger_reckey"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmline_no"].DefaultValue = line.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactnum"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_recr_gljrn.aspx'</script>");
            }
        }
        catch { }
    }

    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

        TextBox line = (TextBox)FormView2.FindControl("lineTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");
        TextBox dscr = (TextBox)FormView2.FindControl("dscrTextBox");
        TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];

        try
        {

            ledger val = new ledger();

            bool check = val.validateGeneralLedgerView("ValidateAdd", "", UserLogin.UserName, Convert.ToInt32(line.Text.Trim()), act.Text.Trim(), Convert.ToString(Session["general_ledger_reckey_journal"]), dscr.Text.Trim(), Convert.ToDecimal(amt.Text.Trim()), 0, reckey.Text.Trim());

            string value = Convert.ToString(check);
            if (value == "True")
            {

                Session["view_recrgeneral_ledger_reckey"] = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmline_no"].DefaultValue = line.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactnum"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmamt"].DefaultValue = amt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_recr_gljrn.aspx'</script>");

            }
        }
        catch { }
        

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
            TextBox line = (TextBox)FormView2.FindControl("lineTextBox");
            TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
            TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");
            TextBox dscr = (TextBox)FormView2.FindControl("dscrTextBox");
            TextBox amt = (TextBox)FormView2.FindControl("amtTextBox");
            TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");



            UserClass UserLogin = (UserClass)Session["User"];
            try
            {               
                ledger con = new ledger();
                DataSet ds = new DataSet();
                ds = con.GeneralLedgerView("AddnewRec", "", UserLogin.UserName, 0, "", Convert.ToString(Session["recr_general_ledger_reckey_journal"]), "", 0, 0, "");

                line.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
                act.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
                actdscr.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                dscr.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
                amt.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);

                reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
                GridView1.Visible = false;
                //}


                //else
                //{
                //    FormView2.ChangeMode(FormViewMode.ReadOnly);
                //}
            }
            catch { }

        }
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
            act.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["view_recrgeneral_ledger_reckey"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;

            try
            {
                if (FormView2.DataItemCount.ToString() == "0")
                    AddNewFormView2Button.Visible = true;
                else
                    AddNewFormView2Button.Visible = false;
            }
            catch { }
        }
    }
    //protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    //{
    //    TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        
    //    UserClass UserLogin = (UserClass)Session["User"];
    //    try
    //    {
    //        voucherpay con = new voucherpay();
    //        DataSet ds = new DataSet();
    //        ds = con.SelectViewDbCrMemo("CancelDelete", "001", UserLogin.UserName, "", "", 0, 0, 0, "", "", reckey.Text.Trim(), "", "");
    //    }
    //    catch { }
    //    FormView1.Visible = true;
    //    FormView1.ChangeMode(FormViewMode.ReadOnly);
    //    Response.Write("<script>window.location.href='view_debit_memo.aspx'</script>");
    //}
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {

        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyLabel");


        ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "DataDelete";
        ObjectDataSource3.SelectParameters["prmactdscr"].DefaultValue = Convert.ToString(Session["recr_general_ledger_reckey_journal"]);
        Response.Write("<script>window.location.href='view_recr_gljrn.aspx'</script>");
            

    }

    protected void InsertCancelButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            ledger con = new ledger();
            DataSet ds = new DataSet();
            ds = con.GeneralLedgerView("DataDelete", "001", UserLogin.UserName, 0, "",Convert.ToString(Session["recr_general_ledger_reckey_journal"]) , "", 0, 0,  reckey.Text.Trim());
        }
        catch { }        
        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }

    protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        AddNewFormView2Button.Visible = false;
    }
    protected void load_viewjournals_Click(object sender, EventArgs e)
    {
        Response.Redirect("load_recr_journal.aspx");
    }

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

    protected void img_btn_add_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);

    }    
    

}
