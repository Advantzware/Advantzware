
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

public partial class view_misc_cshrcpt : System.Web.UI.Page
{
    string chkno = "";
    string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_misc_cshrcpt.aspx";
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

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {

                if (Convert.ToString(Session["misc_cash_receipt_add_button"]) == "add")
                {
                    FormView1.ChangeMode(FormViewMode.Insert);
                    Session["misc_cash_receipt_add_button"] = null;
                }
                if (Session["view_misc_cash_receipt_cust_reckey_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_misc_cash_receipt_cust_reckey_index"]);
                        Session["view_misc_cash_receipt_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_misc_cash_receipt_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }

            FormView1.DataBind();
            try
            {
                Label chk = (Label)FormView1.FindControl("chknoTextBox");
                chkno = chk.Text;

                ObjectDataSource2.SelectParameters["prmchkno"].DefaultValue = chkno;
                ObjectDataSource3.SelectParameters["prmchkno"].DefaultValue = chkno;
            }
            catch { }

            if (Convert.ToString(Session["misc_cash_receipt_add_button"]) == "add")
            {
                FormView1.ChangeMode(FormViewMode.Insert);
                Session["misc_cash_receipt_add_button"] = "";
            }



        } //  ! Page.IsPostBack

        
        


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {
           TextBox chkno = (TextBox)FormView1.FindControl("chknoTextBox");
           TextBox payr = (TextBox)FormView1.FindControl("pyerTextBox");
          // TextBox chkamt = (TextBox)FormView1.FindControl("memonumTextBox");
           TextBox chkdt = (TextBox)FormView1.FindControl("dateTextBox");
           TextBox bnkcd = (TextBox)FormView1.FindControl("bnkcdTextBox");
           TextBox dscr = (TextBox)FormView1.FindControl("dscrTextBox");
           TextBox rcrd = (TextBox)FormView1.FindControl("recordTextBox");
           TextBox pstd = (TextBox)FormView1.FindControl("pstdTextBox");
           TextBox curr = (TextBox)FormView1.FindControl("currTextBox");
           TextBox exch = (TextBox)FormView1.FindControl("exgTextBox");
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

           payr.Focus();
           
           try
          {
               UserClass UserLogin = (UserClass)Session["User"];
               account con = new account();
               DataSet ds = new DataSet();
               ds = con.MiscellaneousCashReceiptList("Addnew", "", UserLogin.UserName, "", "", 0, "", "", "", "", 0, 0, "", "", "", "");

               chkno.Text = Convert.ToString(ds.Tables[0].Rows[0][0]);
               payr.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);
               chkdt.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);
               bnkcd.Text = Convert.ToString(ds.Tables[0].Rows[0][4]);
               dscr.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
               rcrd.Text = Convert.ToString(ds.Tables[0].Rows[0][8]);
               pstd.Text = Convert.ToString(ds.Tables[0].Rows[0][9]);
               curr.Text = Convert.ToString(ds.Tables[0].Rows[0][6]);
               exch.Text = Convert.ToString(ds.Tables[0].Rows[0][7]);
               
               reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
               
              
           }
           catch { }
           
           FormView2.Visible = false;
           GridView1.Visible = false;
           AddNewFormView2Button.Visible = false;

       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {
           TextBox payr = (TextBox)FormView1.FindControl("pyerTextBox");
           payr.Focus();
           FormView2.Visible = false;
           GridView1.Visible = false;
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {           
           
           try
           {
               
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["misc_cash_receipt_cust_reckey_rec"] = reckey.Text.Trim();

               ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
               
           }
           catch { }
           
           //Label chk = (Label)FormView1.FindControl("chknoTextBox");
           //chkno = chk.Text;
           //Response.Write(chkno);
           //ObjectDataSource2.SelectParameters["prmchkno"].DefaultValue = chkno;

           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }
   
   protected void delete_Button_Click(object sender, EventArgs e)
   {

       Label reckey = (Label)FormView1.FindControl("reckeyLabel");
       Label rcrd = (Label)FormView1.FindControl("recordTextBox");
       UserClass UserLogin = (UserClass)Session["User"];

       try
       {
            account val = new account();

            bool check = val.validMiscellaneousList("ValidateDelete", "", UserLogin.UserName, "", "", 0, "", "", "", "", 0, 0, "", "", "", Convert.ToString(Session["misc_cash_receipt_cust_reckey_rec"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "DataDelete";
                ObjectDataSource1.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmrcrd"].DefaultValue = rcrd.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);

                Response.Write("<script>window.location.href='view_misc_cshrcpt.aspx'</script>");
            }
       }
       catch { }

   }

   protected void Formview1_InsertCancelButtonClick(object sender, EventArgs e)
   {
       try
       {
           TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");
           UserClass UserLogin = (UserClass)Session["User"];
           account con = new account();
           DataSet ds = new DataSet();           
           ds = con.MiscellaneousCashReceiptList("DataDelete", "", UserLogin.UserName, "", "", 0, "", "", "", "", 0, 0, "", "", "", reckey.Text.Trim());
       }
       catch { }

   }

   protected void UpdateButton_Click(object sender, EventArgs e)
   {
       TextBox chkno = (TextBox)FormView1.FindControl("chknoTextBox");
       TextBox payr = (TextBox)FormView1.FindControl("pyerTextBox");
       // TextBox chkamt = (TextBox)FormView1.FindControl("memonumTextBox");
       TextBox chkdt = (TextBox)FormView1.FindControl("dateTextBox");
       TextBox bnkcd = (TextBox)FormView1.FindControl("bnkcdTextBox");
       TextBox dscr = (TextBox)FormView1.FindControl("dscrTextBox");
       TextBox rcrd = (TextBox)FormView1.FindControl("recordTextBox");
       TextBox pstd = (TextBox)FormView1.FindControl("pstdTextBox");
       TextBox curr = (TextBox)FormView1.FindControl("currTextBox");
       TextBox exch = (TextBox)FormView1.FindControl("exgTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

       if (chkno.Text == "")
           chkno.Text = "0";       

       UserClass UserLogin = (UserClass)Session["User"];
       
       account val = new account();

       bool check = val.validMiscellaneousList("ValidateUpdate", "", UserLogin.UserName, chkno.Text.Trim(), payr.Text.Trim(), 0, "", bnkcd.Text.Trim(), "", curr.Text.Trim(), 0, Convert.ToInt32(rcrd.Text.Trim()), "", "", "", Convert.ToString(Session["misc_cash_receipt_cust_reckey_rec"]));

            string value = Convert.ToString(check);
            if (value == "True")
            {
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
                ObjectDataSource1.SelectParameters["prmchkno"].DefaultValue = chkno.Text.Trim();
                ObjectDataSource1.SelectParameters["prmpayr"].DefaultValue = payr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmchkdt"].DefaultValue = chkdt.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbnkcd"].DefaultValue = bnkcd.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmrcrd"].DefaultValue = rcrd.Text.Trim();
                ObjectDataSource1.SelectParameters["prmpstd"].DefaultValue = pstd.Text.Trim();                
                ObjectDataSource1.SelectParameters["prmcur_cod"].DefaultValue = curr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmex_rate"].DefaultValue = exch.Text.Trim();


                FormView1.ChangeMode(FormViewMode.ReadOnly);
            }
       

   }

   protected void InsertButton_Click(object sender, EventArgs e)
   {
       TextBox chkno = (TextBox)FormView1.FindControl("chknoTextBox");
       TextBox payr = (TextBox)FormView1.FindControl("pyerTextBox");
       // TextBox chkamt = (TextBox)FormView1.FindControl("memonumTextBox");
       TextBox chkdt = (TextBox)FormView1.FindControl("dateTextBox");
       TextBox bnkcd = (TextBox)FormView1.FindControl("bnkcdTextBox");
       TextBox dscr = (TextBox)FormView1.FindControl("dscrTextBox");
       TextBox rcrd = (TextBox)FormView1.FindControl("recordTextBox");
       TextBox pstd = (TextBox)FormView1.FindControl("pstdTextBox");
       TextBox curr = (TextBox)FormView1.FindControl("currTextBox");
       TextBox exch = (TextBox)FormView1.FindControl("exgTextBox");
       TextBox reckey = (TextBox)FormView1.FindControl("Reckey_TextBox");

       if (payr.Text == "")
           payr.Text = "0";
       
       UserClass UserLogin = (UserClass)Session["User"];
       
       account val = new account();

       bool check = val.validMiscellaneousList("ValidateAdd", "", UserLogin.UserName, chkno.Text.Trim(), payr.Text.Trim(), 0, "", bnkcd.Text.Trim(), "", curr.Text.Trim(), 0, Convert.ToInt32(rcrd.Text.Trim()) , "", "", "", "");

            string value = Convert.ToString(check);
            if (value == "True")
            {
                Session["misc_cash_receipt_cust_reckey_rec"] = reckey.Text.Trim();
                Session["view_misc_cash_receipt_cust_reckey"] = reckey.Text.Trim();
                ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource1.SelectParameters["prmchkno"].DefaultValue = chkno.Text.Trim();
                ObjectDataSource1.SelectParameters["prmpayr"].DefaultValue = payr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmchkdt"].DefaultValue = chkdt.Text.Trim();
                ObjectDataSource1.SelectParameters["prmbnkcd"].DefaultValue = bnkcd.Text.Trim();
                ObjectDataSource1.SelectParameters["prmdscr"].DefaultValue = dscr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmrcrd"].DefaultValue = rcrd.Text.Trim();
                ObjectDataSource1.SelectParameters["prmpstd"].DefaultValue = pstd.Text.Trim();
                ObjectDataSource1.SelectParameters["prmcur_cod"].DefaultValue = curr.Text.Trim();
                ObjectDataSource1.SelectParameters["prmex_rate"].DefaultValue = exch.Text.Trim();

                FormView1.ChangeMode(FormViewMode.ReadOnly);
                FormView2.ChangeMode(FormViewMode.Edit);

            }
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
        Response.Redirect("view_misc_cshrcpt.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("misc_cshrcpt_list.aspx");
    }

    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_misc_cash_receipt_cust_reckey_index"] = GridView1.SelectedIndex;
        Session["view_misc_cash_receipt_cust_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
       // TextBox chkno = (TextBox)FormView1.FindControl("chknoTextBox");
        TextBox chkamt = (TextBox)FormView2.FindControl("amtTextBox");        
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");
        

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (chkamt.Text == "")
            chkamt.Text = "0.00";
        
        UserClass UserLogin = (UserClass)Session["User"];
        
        account val = new account();

        bool check = val.validMiscellaneousView("ValidateUpdate", "", UserLogin.UserName, "", "", 0, act.Text.Trim(), "", Convert.ToString(Session["misc_cash_receipt_cust_reckey_rec"]), Convert.ToString(Session["view_misc_cash_receipt_cust_reckey"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
               // ObjectDataSource3.SelectParameters["prmchkno"].DefaultValue = chkno.Text.Trim(); 
                ObjectDataSource3.SelectParameters["prmchkamt"].DefaultValue = chkamt.Text.Trim();                 
                ObjectDataSource3.SelectParameters["prmactno"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactdscr"].DefaultValue = actdscr.Text.Trim();                
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_misc_cshrcpt.aspx'</script>");
            }
    }
    
    protected void AddButton_Formview2_Click(object sender, EventArgs e)
    {

      //  TextBox chkno = (TextBox)FormView1.FindControl("chknoTextBox");
        TextBox chkamt = (TextBox)FormView2.FindControl("amtTextBox");
        TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
        TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");


        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (chkamt.Text == "")
            chkamt.Text = "0.00";

        UserClass UserLogin = (UserClass)Session["User"];

        account val = new account();

        bool check = val.validMiscellaneousView("ValidateAdd", "", UserLogin.UserName, "", "", 0, act.Text.Trim(), "", Convert.ToString(Session["misc_cash_receipt_cust_reckey_rec"]), Convert.ToString(Session["view_misc_cash_receipt_cust_reckey"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {

            Session["view_misc_cash_receipt_cust_reckey"] = reckey.Text.Trim();
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Add";
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
              //  ObjectDataSource3.SelectParameters["prmchkno"].DefaultValue = chkno.Text.Trim(); 
                ObjectDataSource3.SelectParameters["prmchkamt"].DefaultValue = chkamt.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactno"].DefaultValue = act.Text.Trim();
                ObjectDataSource3.SelectParameters["prmactdscr"].DefaultValue = actdscr.Text.Trim();
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_misc_cshrcpt.aspx'</script>");

            }
        

    }

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
           // TextBox chkno = (TextBox)FormView1.FindControl("chknoTextBox");
            TextBox chkamt = (TextBox)FormView2.FindControl("amtTextBox");
            TextBox act = (TextBox)FormView2.FindControl("actnumTextBox");
            TextBox actdscr = (TextBox)FormView2.FindControl("actdscrLabel");


            TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
            act.Focus();

            

            UserClass UserLogin = (UserClass)Session["User"];
            try
            {
                account val = new account();
                DataSet ds = new DataSet();
                ds = val.MiscellaneousCashReceiptView("AddNew", "", UserLogin.UserName, "", "", 0, "", "", Convert.ToString(Session["misc_cash_receipt_cust_reckey_rec"]), "");


                    chkamt.Text = Convert.ToString(ds.Tables[0].Rows[0][1]);                    
                    act.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
                    actdscr.Text = Convert.ToString(ds.Tables[0].Rows[0][3]);                    

                    reckey.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);
                    GridView1.Visible = false;
                
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
                Label reckey = (Label)FormView2.FindControl("ReckeyTextBox");
                Session["view_misc_cash_receipt_cust_reckey"] = reckey.Text.Trim();

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
    protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            account con = new account();
            DataSet ds = new DataSet();
            ds = con.MiscellaneousCashReceiptView("AddDelete", "", UserLogin.UserName, "", "", 0, "", "", "", reckey.Text.Trim());
        }
        catch { }
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='view_misc_cshrcpt.aspx'</script>");
    }
    protected void deleteButton_FormView2_Click(object sender, EventArgs e)
    {
       
        UserClass UserLogin = (UserClass)Session["User"];
        Label reckey = (Label)FormView2.FindControl("ReckeyTextBox");

        
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "DataDelete";
                ObjectDataSource3.SelectParameters["prmOut"].DefaultValue = Convert.ToString(Session["misc_cash_receipt_cust_reckey_rec"]);
                Response.Write("<script>window.location.href='view_misc_cshrcpt.aspx'</script>");
            

    }

    protected void AddNewFormView2Button_Click(object sender, EventArgs e)
    {
        FormView2.ChangeMode(FormViewMode.Insert);
        AddNewFormView2Button.Visible = false;
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
